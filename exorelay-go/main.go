package exorelay

type ExoRelay struct{}

func New(url string, config map[string]interface{}) *ExoRelay {
	return new(ExoRelay)
}

func (*ExoRelay) Connect() {
}
